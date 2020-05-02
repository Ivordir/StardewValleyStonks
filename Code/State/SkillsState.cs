using System;
using System.ComponentModel;

namespace StardewValleyStonks
{
    public class SkillsState : INotifyPropertyChanged
    {
        public int FarmLvl { get; set; }
        public int FarmBuff { get; set; }
        public int BuffedFarmLvl => FarmLvl + FarmBuff;

        public double[] Quality { get; } 

        public MultiplierProfession Agriculturist { get; }
        public PriceProfession Artisan { get; }
        public PriceProfession Tiller { get; }

        public SkillsState()
        {
            FarmLvl = 0;
            FarmBuff = 0;

            Quality = new double[] { 1, 1.25, 1.5, 2 };

            Tiller = new PriceProfession(
                1.1,
                new FarmLvlCondition(5),
                new IProfession[] { Agriculturist, Artisan });

            FarmLvlCondition lvl10 = new FarmLvlCondition(10);
            IProfession[] NeedsTiller = new IProfession[] { Tiller };
            Artisan = new PriceProfession(
                1.4,
                lvl10,
                null,
                NeedsTiller,
                new IProfession[] { Agriculturist });
            Agriculturist = new MultiplierProfession(
                0.1,
                lvl10,
                null,
                NeedsTiller,
                new IProfession[] { Artisan });
        }
    }
}
