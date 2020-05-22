namespace StardewValleyStonks
{
    public class MultiplierProfession : Profession, IMultiplier
    {
        public double Value { get; }

        public MultiplierProfession(
            string name,
            double value,
            Skills skills,
            ICondition[] conditions, 
            Profession[] dependants = null, 
            Profession[] requirements = null, 
            Profession[] exclusiveWith = null) 
            : base(name, skills, conditions, dependants, requirements, exclusiveWith)
        {
            Value = value;
        }
    }
}
