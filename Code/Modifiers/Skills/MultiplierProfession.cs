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
            Profession[] dependants = null, 
            Profession[] requirements = null, 
            Profession[] exclusiveWith = null) 
            : base(name, skill, conditions, dependants, requirements, exclusiveWith)
        {
            Value = value;
        }

        public MultiplierProfession() : base() { }
    }
}
