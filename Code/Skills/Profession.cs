namespace StardewValleyStonks
{
    public class Profession : IProfession
    {
        private static readonly IProfession[] None = new IProfession[0];

        public Profession(
            ICondition lvlCondition,
            IProfession[] dependants = null,
            IProfession[] requirements = null,
            IProfession[] exclusiveWith = null)
        {
            Enabled = false;
            LvlCondition = lvlCondition;
            Dependants = dependants ?? None;
            Requirements = requirements ?? None;
            ExclusiveWith = exclusiveWith ?? None;
        }

        public ICondition LvlCondition { get; }

        public bool Active => Enabled && LvlCondition.IsMet;

        IProfession[] Dependants { get; }
        IProfession[] Requirements { get; }
        IProfession[] ExclusiveWith { get; }

        public bool Enabled
        {
            get => Enabled;
            set
            {
                Enabled = value;
                if (Enabled)
                {
                    SetAll(Requirements, true);
                    SetAll(ExclusiveWith, false);
                }
                else
                {
                    SetAll(Dependants, false);
                }
            }
        }

        private void SetAll(IProfession[] professions, bool enabled)
        {
            if (professions != None)
            {
                foreach (IProfession profession in professions)
                {
                    profession.Enabled = enabled;
                }
            }
        }
    }
}
