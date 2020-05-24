using System.Collections.Generic;

namespace StardewValleyStonks
{
    public abstract class Selectable : ISelectable, IWarn
    {
        public virtual bool Selected { get; set; }
        public virtual bool Active => Selected && ConditionsMet;
        public virtual List<Warning> Warnings
        {
            get
            {
                _Warnings.Clear();
                if (Conditions != null)
                {
                    foreach (ICondition condition in Conditions)
                    {
                        if (!condition.IsMet)
                        {
                            _Warnings.Add(condition.Warning);
                        }
                    }
                }
                if (!Selected)
                {
                    _Warnings.Add(NotSelected);
                }
                return _Warnings;
            }
        }
        public string DisplayWarnings
        {
            get
            {
                string display = "";
                foreach (Warning warning in Warnings)
                {
                    display += warning.Display() + "\n";
                }
                return display;
            }
        }

        private readonly ICondition[] Conditions;
        private readonly List<Warning> _Warnings;
        private bool ConditionsMet
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
            _Warnings = new List<Warning>();
        }

        //initialize selected in derived class or keep default false value
        protected Selectable(ICondition[] conditions = null)
        {
            Conditions = conditions;
            _Warnings = new List<Warning>();
        }

        private static readonly Warning NotSelected = new Warning("Not selected.");
    }
}
