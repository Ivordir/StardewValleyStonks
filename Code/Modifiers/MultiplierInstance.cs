namespace StardewValleyStonks
{
    public class MultiplierInstance : IMultiplier
    {
        public bool Selected { get; set; }
        public string Name => Multiplier.Name;
        public double Value => Multiplier.Value;
        public ICondition[] Conditions => Multiplier.Conditions;
        public bool ConditionsMet => Multiplier.ConditionsMet;
        public bool Active => Selected && Multiplier.ConditionsMet;

        private readonly IMultiplier Multiplier;

        public MultiplierInstance(
            IMultiplier multiplier,
            bool selected = false)
        {
            Multiplier = multiplier;
            Selected = selected;
        }
    }
}
