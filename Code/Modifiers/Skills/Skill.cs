namespace StardewValleyStonks
{
    public class Skill : ISkill
    {
        public Skill(string name, IProfession[][] professions, int level = 0, int buff = 0, bool ignoreConflicts = false)
        {
            Name = name;
            Level = level;
            Buff = buff;
            Professions = professions;
            IgnoreConflicts = ignoreConflicts;
        }

        public string Name { get; }
        public int Level
        {
            get => Level;
            set => value.InRange(0, 10);
        }
        public int Buff
        {
            get => Buff;
            set => value.WithMin(0);
        }
        public int BuffedLevel => Level + Buff;
        public IProfession[][] Professions { get; }
        public bool IgnoreConflicts { get; set; }
    }
}
