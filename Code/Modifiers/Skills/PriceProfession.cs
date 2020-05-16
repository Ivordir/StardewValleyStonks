namespace StardewValleyStonks
{
    public class PriceProfession : Profession, IPriceMultiplier
    {
        public double Value { get; }

        public PriceProfession(
            string name,
            double multiplier,
            ICondition[] conditions,
            IProfession[] dependants = null,
            IProfession[] requirements = null,
            IProfession[] exclusiveWith = null)
            : base(name, conditions, dependants, requirements, exclusiveWith)
        {
            Value = multiplier;
        }

        public int ApplyTo(int basePrice) => Active ? (int)(Value * basePrice) : basePrice; 
    }
}
