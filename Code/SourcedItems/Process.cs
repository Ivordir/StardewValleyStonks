﻿using System;

namespace StardewValleyStonks
{
    public class Process : Selectable, IProcess
    {
        public static Func<IProcess, IProcess, int>[] Comparers { get; }

        static Process()
        {
            Comparers = new Func<IProcess, IProcess, int>[4];
            for (int quality = 0; quality < 4; quality++)
            {
                Comparers[quality] = new Func<IProcess, IProcess, int>((p1, p2) => p1.CompareTo(p2, quality));
            }
        }

        public Item Input { get; }
        public int InputAmount => 1;
        public Processor Source { get; }
        public double OutputAmount => 1;
        public override bool Active => base.Active && Source.Active;

        readonly Item _Output;

        public IItem Output(IItem input) => Output(input.Quality);
        public double ProfitPerInput(int quality) => Output(quality).Price;
        public int CompareTo(IProcess other, int quality) =>
            ProfitPerInput(quality).CompareTo(other.ProfitPerInput(quality));

        public double Profit(double output) => Output.Price * output;
        public double MaxOutput(QualityDist inputs)
            => inputs.AllQualities / InputAmount * OutputAmount;

        private IItem Output(int quality) =>
            Source.PreservesQuality ?
            _Output.WithQuality[quality] :
            _Output.Normal;

        public Process(
            Item item,
            Processor sellSource,
            ICondition[] conditions = null)
            : base(true, conditions)
        {
            Input = item;
            Source = sellSource;
            _Output = item;
        }
        public Process(
            Item input,
            Processor processor,
            Item output,
            ICondition[] conditions = null)
            : base(true, conditions)
        {
            Input = input;
            Source = processor;
            _Output = output;
        }
    }
}
