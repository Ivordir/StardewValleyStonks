namespace StardewValleyStonks
{
    public class Multiplier : Profession, IMultiplier
    {
        public double Value { get; }

        public Multiplier(
            double value,
            ICondition lvlCondition,
            IProfession[] dependants = null,
            IProfession[] requirements = null,
            IProfession[] exclusiveWith = null)
            : base(lvlCondition, dependants, requirements, exclusiveWith)
        {
            Value = value;
        }
    }
}
