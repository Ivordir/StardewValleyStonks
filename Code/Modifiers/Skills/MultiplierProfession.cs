namespace StardewValleyStonks
{
    public class MultiplierProfession : Profession, IMultiplier
    {
        public double Value { get; }

        public MultiplierProfession(
            string name,
            double value,
            Skill skill,
            ICondition[] conditions, 
            IProfession[] dependants = null, 
            IProfession[] requirements = null, 
            IProfession[] exclusiveWith = null) 
            : base(name, skill, conditions, dependants, requirements, exclusiveWith)
        {
            Value = value;
        }
    }
}
