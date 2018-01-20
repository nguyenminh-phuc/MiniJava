using System.Collections.Generic;

namespace MiniJava.Web.ViewModels
{
    public class HomeViewModel
    {
        public IReadOnlyCollection<string> SampleNames { get; set; }

        public HomeItemViewModel Item { get; set; }
    }
}