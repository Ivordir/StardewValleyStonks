using System.Collections.Generic;

namespace StardewValleyStonks
{
    public class ActiveItem : IActiveItem
    {
        private static readonly List<ICondition> NoConditions;

        static ActiveItem()
        {
            NoConditions = new List<ICondition>();
        }

        public bool Enabled { get; set; }
        public List<ICondition> Conditions { get; }

        public ActiveItem(bool enabled = true, List<ICondition> conditions = null)
        {
            Enabled = enabled;
            Conditions = conditions ?? NoConditions;
        }

        public bool ConditionsMet
        {
            get
            {
                foreach (ICondition condition in Conditions)
                {
                    if (!condition.IsMet)
                    {
                        return false;
                    }
                }
                return true;
            }
        }

        public bool Active
        {
            get
            {
                return Enabled && ConditionsMet;
            }
        }
    }
}
