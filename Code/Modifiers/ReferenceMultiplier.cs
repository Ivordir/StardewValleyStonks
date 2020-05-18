namespace StardewValleyStonks
{
    public class ReferenceMultiplier : IMultiplier
    {
        public string Name => Multiplier.Name;
        public double Value => Multiplier.Value;
        public bool Selected { get; set; }
        public ICondition[] Conditions => Multiplier.Conditions;
        public bool ConditionsMet => Multiplier.ConditionsMet;
        public bool Active => Selected && Multiplier.ConditionsMet;

        private readonly IMultiplier Multiplier;

        public ReferenceMultiplier(
            IMultiplier multiplier,
            bool selected = false)
        {
            Multiplier = multiplier;
            Selected = selected;
        }
    }
}
