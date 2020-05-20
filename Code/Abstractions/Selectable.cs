namespace StardewValleyStonks
{
    public abstract class Selectable : ISelectable
    {
        public virtual bool Selected { get; set; }
        public ICondition[] Conditions { get; }

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

        public Selectable(bool selected = true, ICondition[] conditions = null)
        {
            Selected = selected;
            Conditions = conditions ?? None;
        }

        //initialize selected in derived class or keep default false value
        protected Selectable(ICondition[] conditions = null)
        {
            Conditions = conditions ?? None;
        }

        private static readonly ICondition[] None = new ICondition[0];
    }
}
