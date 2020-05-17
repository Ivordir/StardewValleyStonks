namespace StardewValleyStonks
{
    public class PriceProfession : BaseProfession, IPriceMultiplier
    {
        public override string Name => PriceMultiplier.Name;
        public double Value => PriceMultiplier.Value;

        private readonly IPriceMultiplier PriceMultiplier;

        public PriceProfession(
            IPriceMultiplier priceMultiplier,
            Skill skill,
            ICondition[] conditions,
            IProfession[] dependants = null,
            IProfession[] requirements = null,
            IProfession[] exclusiveWith = null)
            : base(skill, conditions, dependants, requirements, exclusiveWith)
        {
            PriceMultiplier = priceMultiplier;
        }

        public int ApplyTo(int basePrice) => Active ? (int)(Value * basePrice) : basePrice; 
    }
}
