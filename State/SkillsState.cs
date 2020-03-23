namespace StardewValleyStonks
{
    public class SkillsState
    {
        public int FarmLvl { get; set; } = 0;
        public int FarmBuff { get; set; } = 0;
        public int BuffedFarmLvl => FarmLvl + FarmBuff;
        public bool Till { get; set; } = false;
        public bool Agri { get; set; } = false;
        public bool Arti { get; set; } = false;
    }
}
