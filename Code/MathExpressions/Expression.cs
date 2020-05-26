using System.Linq;

namespace StardewValleyStonks
{
    public readonly struct Expression : IValue
    {
        public readonly double Value => Terms.Sum(x => x.Value);

        private readonly IValue[] Terms;

        public Expression(IValue[] terms)
        {
            Terms = terms;
        }
    }
}
