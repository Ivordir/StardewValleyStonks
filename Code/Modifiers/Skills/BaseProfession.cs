namespace StardewValleyStonks
{
    public abstract class BaseProfession : Selectable, IProfession
    {
        private static readonly IProfession[] None = new IProfession[0];

        public abstract string Name { get; }
        public IProfession[] Dependants { get; }
        public IProfession[] Requirements { get; }
        public IProfession[] ExclusiveWith { get; }

        private readonly Skill Skill;
        public override bool Selected
        {
            get => base.Selected;
            set
            {
                base.Selected = value;
                if (!Skill.IgnoreConflicts)
                {
                    if (value)
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
        }

        public BaseProfession(
            Skill skill,
            ICondition[] conditions,
            IProfession[] dependants = null,
            IProfession[] requirements = null,
            IProfession[] exclusiveWith = null)
            : base (conditions)
        {
            Skill = skill;
            Dependants = dependants ?? None;
            Requirements = requirements ?? None;
            ExclusiveWith = exclusiveWith ?? None;
        }

        private void SetAll(IProfession[] professions, bool selected)
        {
            if (professions != None)
            {
                foreach (IProfession profession in professions)
                {
                    profession.Selected = selected;
                }
            }
        }
    }
}
