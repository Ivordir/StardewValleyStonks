namespace StardewValleyStonks
{
    public abstract class Selectable : ISelectable, IWarn
    {
        public virtual bool Selected { get; set; }
        public virtual bool Active => Selected && ConditionsMet;
        public virtual string Warnings
        {
            get
            {
                string warnings = string.Empty;
                if (Conditions != null)
                {
                    foreach (ICondition condition in Conditions)
                    {
                        if (!condition.IsMet)
                        {
                            warnings += condition.Warning.WrapTag("li");
                        }
                    }
                }
                if (!Selected)
                {
                    warnings += "Not selected.".WrapTag("li");
                }
                return warnings.WrapTag("ul");
            }
        }

        readonly ICondition[] Conditions;
        bool ConditionsMet
        {
            get
            {
                if (Conditions != null)
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

        public Selectable(
            bool selected,
            ICondition[] conditions = null)
        {
            Selected = selected;
            Conditions = conditions;
        }

        // initialize selected in derived class or keep default false value
        protected Selectable(ICondition[] conditions = null)
        {
            Conditions = conditions;
        }
    }
}
