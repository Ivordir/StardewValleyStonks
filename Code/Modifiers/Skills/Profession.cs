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
                if (!Skills.IgnoreConflicts)
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

        private readonly Skills Skills;
        private readonly Profession[] Dependants, Requirements, ExclusiveWith;

        private void SetAll(Profession[] professions, bool selected)
        {
            if (professions != null)
            {
                foreach (Profession profession in professions)
                {
                    profession.Selected = selected;
                }
            }
        }

        public Profession(
            string name,
            Skills skills,
            ICondition[] conditions,
            Profession[] dependants = null,
            Profession[] requirements = null,
            Profession[] exclusiveWith = null)
            : base (conditions)
        {
            Name = name;
            Skills = skills;
            Dependants = dependants;
            Requirements = requirements;
            ExclusiveWith = exclusiveWith;
        }
    }
}
