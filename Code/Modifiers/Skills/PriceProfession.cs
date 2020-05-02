namespace StardewValleyStonks
{
    public class PriceProfession : Profession, IPriceMultiplier
    {
        public double Value { get; }

        public PriceProfession(
            double multiplier,
            ICondition[] conditions,
            IProfession[] dependants = null,
            IProfession[] requirements = null,
            IProfession[] exclusiveWith = null)
            : base(conditions, dependants, requirements, exclusiveWith)
        {
            Value = multiplier;
        }

        public int ApplyTo(int basePrice) => Active ? (int)(Value * basePrice) : basePrice; 
    }
}
