using System;

namespace StardewValleyStonks
{
    public class Process : Selectable, IProcess
    {
        public static Func<Process, Process, int>[] Comparers { get; }

        static Process()
        {
            Comparers = new Func<IProcess, IProcess, int>[4];
            for (int quality = 0; quality < 4; quality++)
            {
                Comparers[quality] = new Func<IProcess, IProcess, int>((p1, p2) => p1.CompareTo(p2, quality));
            }
        }

        public Item Input { get; }
        public virtual int InputAmount => 1;
        public Processor Source { get; }
        public Item OutputItem { get; }
        public override bool Active => base.Active && Source.Active;

        public QualityItem Output(QualityItem input) => Output(input.Quality);
        public int CompareTo(IProcess other, int quality)
        => ProfitPerInput(quality).CompareTo(other.ProfitPerInput(quality));

        //public double Profit(double output) => Output.Price * output;
        public virtual double OutputAmount(QualityItem input) => 1;
        public virtual double ProfitPerInput(int quality) => Output(quality).Price;
        public virtual double MaxOutput(QualityDist inputs) => inputs.Value;

        protected QualityItem Output(int quality)
        => Source.PreservesQuality
            ? OutputItem.With(quality)
            : OutputItem.Normal;

        public Process(
            Item item,
            Processor sellSource,
            ICondition[] conditions = null)
            : base(true, conditions)
        {
            Input = item;
            Source = sellSource;
            OutputItem = item;
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
            OutputItem = output;
        }
    }
}
