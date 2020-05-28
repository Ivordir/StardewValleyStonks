﻿namespace StardewValleyStonks
{
    public interface IProcess : ISelectable
    {
        public Item Input { get; }
        public int InputAmount { get; }
        public Processor Source { get; }
        public double OutputAmount { get; }

        public IItem Output(IItem input);
        public double ProfitPerInput(int quality);
        public int CompareTo(IProcess other, int quality);
    }
}
