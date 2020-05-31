namespace StardewValleyStonks
{
    public class MultiplierProfession : Profession, IMultiplier
    {
        public double Value { get; }
        public override bool Active => _Active;

        //temporary way to save whether a multiplier applies to items used in output.
        public void Save()
        {
            _Active = base.Active;
        }

        bool _Active;

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
