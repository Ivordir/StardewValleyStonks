namespace StardewValleyStonks
{
    public class Profession : Selectable, IProfession
    {
        private static readonly IProfession[] None = new IProfession[0];

        public string Name { get; }

        public Profession(
            string name,
            ICondition[] conditions,
            IProfession[] dependants = null,
            IProfession[] requirements = null,
            IProfession[] exclusiveWith = null)
            : base (false, conditions)
        {
            Name = name;
            Dependants = dependants ?? None;
            Requirements = requirements ?? None;
            ExclusiveWith = exclusiveWith ?? None;
        }

        private readonly ISkill Skill;
        public IProfession[] Dependants { get; }
        public IProfession[] Requirements { get; }
        public IProfession[] ExclusiveWith { get; }

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
