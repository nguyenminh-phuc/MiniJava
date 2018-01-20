using System.IO;
using System.IO.Compression;
using System.Linq;
using System.Net.Mime;
using Microsoft.AspNetCore.Hosting;
using Microsoft.AspNetCore.Mvc;
using Microsoft.Extensions.FileProviders;
using Microsoft.FSharp.Core;
using MiniJava.Web.Interfaces;
using MiniJava.Web.ViewModels;
using Newtonsoft.Json;

namespace MiniJava.Web.Controllers
{
    public class HomeController : Controller
    {
        private const string DefaultSample = "Factorial.java";

        private readonly INodeBuilderService<MiniJava.Program> _astBuilder;

        private readonly INodeBuilderService<Assembly> _ilBuilder;

        private readonly IDirectoryContents _paths;

        public HomeController(
            IHostingEnvironment env,
            INodeBuilderService<MiniJava.Program> astBuilder,
            INodeBuilderService<Assembly> ilBuilder)
        {
            _paths = env.WebRootFileProvider.GetDirectoryContents("sources");
            _astBuilder = astBuilder;
            _ilBuilder = ilBuilder;
        }

        [HttpGet]
        public ViewResult Index()
        {
            var samplePaths = _paths.Where(p => p.Name.EndsWith(".java")).ToList();
            var source = System.IO.File.ReadAllText(samplePaths.Single(s => s.Name == DefaultSample).PhysicalPath);

            var vm = new HomeViewModel
            {
                SampleNames = samplePaths.Select(p => p.Name).ToList(),
                Item = Parse(source, 1000)
            };

            return View(vm);
        }

        [HttpPost]
        public ActionResult DownloadSource(string source, long timer)
        {
            var vm = Parse(source, timer, true);

            if (vm.Application == null) return RedirectToAction("Index");

            using (var ms = new MemoryStream())
            {
                using (var archive = new ZipArchive(ms, ZipArchiveMode.Create, true))
                {
                    var assemblyFile = archive.CreateEntry(vm.Application.assemblyName);
                    using (var assemblyStream = assemblyFile.Open())
                    using (var bw = new BinaryWriter(assemblyStream))
                    {
                        bw.Write(vm.Application.assemblyContents);
                    }

                    var configFile = archive.CreateEntry(vm.Application.configName);
                    using (var configStream = configFile.Open())
                    using (var sw = new StreamWriter(configStream))
                    {
                        sw.Write(vm.Application.configContents);
                    }
                }

                var fileName = $"{vm.Application.assemblyName}.zip";
                return File(ms.ToArray(), MediaTypeNames.Application.Zip, fileName);
            }
        }

        [HttpPost]
        public JsonResult SelectSample(string sampleName, long timer)
        {
            var samplePath = _paths.Single(p => p.Name == sampleName);
            var source = System.IO.File.ReadAllText(samplePath.PhysicalPath);

            var vm = Parse(source, timer);

            return Json(vm);
        }

        [HttpPost]
        public JsonResult ParseSource(string source, long timer)
        {
            var vm = Parse(source, timer);

            return Json(vm);
        }

        public ActionResult Error()
        {
            return View("~/Views/Shared/Error.cshtml");
        }

        private HomeItemViewModel Parse(string source, long timer, bool generateAssembly = false)
        {
            string error = null;

            var vm = new HomeItemViewModel {Source = source, Timer = timer};
            var input = InputModule.create(vm.Source);

            var tokensResult = Lexer.tokenize(input);
            if (tokensResult.IsError)
            {
                error = tokensResult.ErrorValue;
                goto End;
            }

            var tokens = tokensResult.ResultValue;

            var programResult = Parser.parse(tokens);
            if (programResult.IsError)
            {
                error = programResult.ErrorValue;
                goto End;
            }

            var program = programResult.ResultValue;
            vm.Ast = JsonConvert.SerializeObject(_astBuilder.Build(program));

            var symbolTableResult = SymbolCollector.create(program);
            if (symbolTableResult.IsError)
            {
                error = symbolTableResult.ErrorValue;
                goto End;
            }

            var symbolTable = symbolTableResult.ResultValue;

            var typeCheckerResult = TypeChecker.check(symbolTable, program);
            if (typeCheckerResult.IsError)
            {
                error = typeCheckerResult.ErrorValue;
                goto End;
            }

            var variableInitializationCheckerResult = VariableInitializationChecker.check(program);

            if (variableInitializationCheckerResult.IsError)
            {
                error = variableInitializationCheckerResult.ErrorValue;
                goto End;
            }

            var writeLineFunc = FuncConvert.ToFSharpFunc<int>(i => vm.PrintfnLog += i + "\n");
            var environment = EnvironmentModule.create(symbolTable, program, writeLineFunc, vm.Timer);

            var interpreterResult = Interpreter.interpret(environment);
            if (interpreterResult.IsError)
            {
                error = interpreterResult.ErrorValue;
                goto End;
            }

            var assemblyResult = ILBuilder.build(symbolTable, program);
            if (assemblyResult.IsError)
            {
                error = assemblyResult.ErrorValue;
                goto End;
            }

            var assembly = assemblyResult.ResultValue;
            vm.Il = JsonConvert.SerializeObject(_ilBuilder.Build(assembly));

            if (generateAssembly)
            {
                var applicationResult = CodeGenerator.generate(assembly);
                if (applicationResult.IsError)
                {
                    error = applicationResult.ErrorValue;
                    goto End;
                }

                vm.Application = applicationResult.ResultValue;
            }

            End:
            if (error != null) vm.Error = error;

            return vm;
        }
    }
}