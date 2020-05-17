﻿namespace StardewValleyStonks
{
    public class Profession : BaseProfession
    {
        public override string Name { get; }
        
        public Profession(
            string name,
            ICondition[] conditions,
            IProfession[] dependants = null,
            IProfession[] requirements = null,
            IProfession[] exclusiveWith = null)
            : base (conditions, dependants, requirements, exclusiveWith)
        {
            Name = name;
        }
    }
}
