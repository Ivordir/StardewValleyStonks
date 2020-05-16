namespace StardewValleyStonks
{
    public interface ISkill
    {
        public int Buff { get; set; }
        public int BuffedLevel { get; }
        public bool IgnoreConflicts { get; set; }
        public int Level { get; set; }
        public string Name { get; }
        public IProfession[][] Professions { get; }
    }
}