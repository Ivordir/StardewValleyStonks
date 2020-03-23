using System.Collections.Generic;

namespace StardewValleyStonks
{
    public abstract class BaseItem<Child>
    {
        public virtual int Price => BasePrice;
        public bool IsEnabled { get; set; }
        public List<ICondition> Conditions { get; }

        protected readonly int BasePrice;

        public BaseItem(int basePrice, bool enabled, List<ICondition> conditions = null)
        {
            BasePrice = basePrice;
            IsEnabled = enabled;
            Conditions = conditions ?? new List<ICondition>();
        }
        public bool IsValid
        {
            get
            {
                if (!IsEnabled)
                {
                    return false;
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
        public abstract bool IsBetterThan(Child other);
    }
}
