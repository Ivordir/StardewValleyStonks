namespace StardewValleyStonks
{
    public class MultiplierProfession : Profession, IMultiplier
    {
        public MultiplierProfession(
            string name, 
            double value, 
            ICondition[] conditions, 
            IProfession[] dependants = null, 
            IProfession[] requirements = null, 
            IProfession[] exclusiveWith = null) 
            : base(name, conditions, dependants, requirements, exclusiveWith)
        {
            Value = value;
        }

        public double Value { get; }
    }
}
