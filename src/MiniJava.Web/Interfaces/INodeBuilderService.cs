using System.Collections.Generic;
using MiniJava.Web.Models;

namespace MiniJava.Web.Interfaces
{
    public interface INodeBuilderService<in T>
    {
        IReadOnlyCollection<Node> Build(T root);
    }
}