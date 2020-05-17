namespace StardewValleyStonks
{
    public class MultiplierProfession : BaseProfession, IMultiplier
    {
        public override string Name => Multiplier.Name;
        public double Value => Active ? Multiplier.Value : DefaultValue;

        private readonly IMultiplier Multiplier;
        private readonly double DefaultValue;

        public MultiplierProfession(
            IMultiplier multiplier,
            double defaultValue,
            ICondition[] conditions, 
            IProfession[] dependants = null, 
            IProfession[] requirements = null, 
            IProfession[] exclusiveWith = null) 
            : base(conditions, dependants, requirements, exclusiveWith)
        {
            Multiplier = multiplier;
            DefaultValue = defaultValue;
        }
    }
}
