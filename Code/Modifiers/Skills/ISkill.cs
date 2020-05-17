namespace StardewValleyStonks
{
    public interface ISkill
    {
        public string Name { get; }
        public int Level { get; set; }
        public int Buff { get; set; }
        public int BuffedLevel { get; }
        public IProfession[][] Professions { get; }
        public bool IgnoreConflicts { get; set; }
    }
}