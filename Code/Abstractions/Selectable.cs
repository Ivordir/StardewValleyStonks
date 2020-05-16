namespace StardewValleyStonks
{
    public abstract class Selectable : ISelectable
    {
        private static readonly ICondition[] None = new ICondition[0];

        public virtual bool Selected { get; set; }
        public ICondition[] Conditions { get; }

        public Selectable(bool selected = true, ICondition[] conditions = null)
        {
            Selected = selected;
            Conditions = conditions ?? None;
        }

        public bool ConditionsMet
        {
            get
            {
                if (Conditions != None)
                {
                    foreach (ICondition condition in Conditions)
                    {
                        if (!condition.IsMet)
                        {
                            return false;
                        }
                    }
                }
                return true;
            }
        }

        public virtual bool Active => Selected && ConditionsMet;
    }
}
