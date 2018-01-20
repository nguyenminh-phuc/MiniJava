using System.Collections.Generic;
using Newtonsoft.Json;

namespace MiniJava.Web.Models
{
    public class Node
    {
        public Node(bool showTags = false)
        {
            ShowTags = showTags;
        }

        [JsonIgnore] public bool ShowTags { get; set; }

        [JsonProperty("text")] public string Text { get; set; }

        [JsonProperty("nodes")] public IList<Node> Nodes { get; set; }

        [JsonProperty("tags")]
        public IList<string> Tags => ShowTags == false
            ? null
            : new List<string> {Nodes == null ? 0.ToString() : Nodes.Count.ToString()};

        public void Add(Node child)
        {
            if (Nodes == null) Nodes = new List<Node>();
            Nodes.Add(child);
        }
    }
}