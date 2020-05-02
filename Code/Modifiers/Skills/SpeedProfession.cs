namespace StardewValleyStonks
{
    public class SpeedProfession : Profession, ISpeedMultiplier
    {
        public double Value { get; }

        public bool HasTableColumn => false;

        public SpeedProfession(
            double value,
            ICondition[] conditions,
            IProfession[] dependants = null,
            IProfession[] requirements = null,
            IProfession[] exclusiveWith = null)
            : base(conditions, dependants, requirements, exclusiveWith)
        {
            Value = value;
        }
    }
}
