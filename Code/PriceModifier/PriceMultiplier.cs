namespace StardewValleyStonks
{
    public class PriceMultiplier : Profession, IPriceMultiplier
    {
        private readonly double Value;

        public PriceMultiplier(
            double multiplier,
            ICondition lvlCondition,
            IProfession[] dependants = null,
            IProfession[] requirements = null,
            IProfession[] exclusiveWith = null)
            : base(lvlCondition, dependants, requirements, exclusiveWith)
        {
            Value = multiplier;
        }

        public int ApplyTo(int basePrice) => Active ? (int)(Value * basePrice) : basePrice; 
    }
}
