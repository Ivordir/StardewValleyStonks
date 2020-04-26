namespace StardewValleyStonks
{
    public abstract class ActiveItem : IActiveItem
    {
        private static readonly ICondition[] NoConditions;

        static ActiveItem()
        {
            NoConditions = new ICondition[0];
        }

        public bool Enabled { get; set; }
        public ICondition[] Conditions { get; }

        public ActiveItem(bool enabled = true, ICondition[] conditions = null)
        {
            Enabled = enabled;
            Conditions = conditions ?? NoConditions;
        }

        public bool ConditionsMet
        {
            get
            {
                if (Conditions == NoConditions)
                {
                    return true;
                }
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

        public virtual bool Active
        {
            get
            {
                return Enabled && ConditionsMet;
            }
        }
    }
}
