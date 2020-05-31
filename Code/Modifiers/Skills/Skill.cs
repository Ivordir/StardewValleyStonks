using ExtentionsLibrary.Limits;

namespace StardewValleyStonks
{
    public class Skill
    {
        public string Name { get; }
        public Profession[][] Professions { get; }
        public int Level
        {
            get => _Level;
            set => _Level = value.InRange(0, 10);
        }
        public int Buff
        {
            get => _Buff;
            set => _Buff = value.WithMin(0);
        }
        public int BuffedLevel => _Level + _Buff;

        int _Level, _Buff;

        public Skill(
            string name,
            Profession[][] professions,
            int level = 0,
            int buff = 0)
        {
            Name = name;
            _Level = level;
            _Buff = buff;
            Professions = professions;
        }
    }
}
