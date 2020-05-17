namespace StardewValleyStonks
{
    public abstract class BaseProfession : Selectable, IProfession
    {
        private static readonly IProfession[] None = new IProfession[0];

        public abstract string Name { get; }
        public IProfession[] Dependants { get; }
        public IProfession[] Requirements { get; }
        public IProfession[] ExclusiveWith { get; }

        private readonly ISkill Skill;
        public override bool Selected
        {
            get => base.Selected;
            set
            {
                Selected = value;
                if (!Skill.IgnoreConflicts)
                {
                    if (Selected)
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
            ICondition[] conditions,
            IProfession[] dependants = null,
            IProfession[] requirements = null,
            IProfession[] exclusiveWith = null)
            : base (false, conditions)
        {
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
