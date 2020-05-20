namespace StardewValleyStonks
{
    public class Profession : Selectable
    {
        public string Name { get; }

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

        private readonly Skill Skill;
        private readonly Profession[] Dependants;
        private readonly Profession[] Requirements;
        private readonly Profession[] ExclusiveWith;

        public Profession(
            string name,
            Skill skill,
            ICondition[] conditions,
            Profession[] dependants = null,
            Profession[] requirements = null,
            Profession[] exclusiveWith = null)
            : base (conditions)
        {
            Name = name;
            Skill = skill;
            Dependants = dependants ?? None;
            Requirements = requirements ?? None;
            ExclusiveWith = exclusiveWith ?? None;
        }

        private void SetAll(Profession[] professions, bool selected)
        {
            if (professions != None)
            {
                foreach (Profession profession in professions)
                {
                    profession.Selected = selected;
                }
            }
        }

        private static readonly Profession[] None = new Profession[0];
    }
}
