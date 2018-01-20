using System.Collections.Generic;
using Newtonsoft.Json;

namespace MiniJava.Web.ViewModels
{
    public class HomeItemViewModel
    {
        [JsonProperty("source")] public string Source { get; set; }

        [JsonProperty("timer")] public long Timer { get; set; }

        [JsonProperty("ast")] public string Ast { get; set; }

        [JsonProperty("il")] public string Il { get; set; }

        [JsonIgnore] public Application Application { get; set; }

        [JsonProperty("printfnLog")] public string PrintfnLog { get; set; }

        [JsonProperty("error")] public string Error { get; set; }

        [JsonProperty("memory")] public IList<string> Memory { get; set; }
    }
}