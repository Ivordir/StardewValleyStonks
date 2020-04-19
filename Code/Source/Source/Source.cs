using System.Collections.Generic;

namespace StardewValleyStonks
{
    public class Source : ActiveItem, ISource
    {
        public string Name { get; }

        public Source(string name, bool enabled = true, List<ICondition> conditions = null) : base(enabled, conditions)
        {
            Name = name;
        }

        public override int GetHashCode()
        {
            return System.HashCode.Combine(Name);
        }
    }
}
