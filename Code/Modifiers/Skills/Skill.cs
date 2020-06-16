using static System.Math;

namespace StardewValleyStonks
{
    public class Skill
    {
        public string Name { get; }
        public Profession[][] Professions { get; }
        public int Level
        {
            get => _Level;
            set => _Level = Clamp(value, 0, 10);
        }
        public int Buff
        {
            get => _Buff;
            set => _Buff = Max(value, 0);
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
