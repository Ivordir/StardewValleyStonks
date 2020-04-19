using System.Collections.Generic;

namespace StardewValleyStonks
{
    public interface IActiveItem
    {
        public bool Enabled { get; set; }
        public List<ICondition> Conditions { get; }

        public bool ConditionsMet { get; }
        public bool Active { get; }
    }
}