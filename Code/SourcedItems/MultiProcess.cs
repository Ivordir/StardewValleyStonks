﻿using System.Collections.Generic;
using System.Linq;
using ExtentionsLibrary.Collections;

namespace StardewValleyStonks
{
    public class MultiProcess : Selectable, INullComparable<MultiProcess>
    {
        public Processor Source { get; }
        public Dictionary<Item, int> Inputs { get; }
        public Item OutputItem => Source.PreservesQuality ? _OutputItem : _OutputItem;
        public double OutputAmount { get; }
        public override bool Active => base.Active && Source.Active;

        public int? CompareTo(MultiProcess other)
        {
            bool superSet = Inputs.IsSuperSetOf(other.Inputs);
            bool subSet = Inputs.IsSubSetOf(other.Inputs);
            if (subSet && superSet) //same inputs
            {
                bool anyBetter = OutputAmount * OutputItem.Price > other.OutputAmount * other.OutputItem.Price;
                bool anyWorse = OutputAmount * OutputItem.Price < other.OutputAmount * other.OutputItem.Price;
                foreach (Item item in Inputs.Keys)
                {
                    if (Inputs[item] > other.Inputs[item])
                    {
                        anyWorse = true;
                    }
                    else if (Inputs[item] < other.Inputs[item])
                    {
                        anyBetter = true;
                    }
                    if (anyBetter && anyWorse)
                    {
                        return null;
                    }
                }
                if (anyBetter)
                {
                    return 1;
                }
                else if (anyWorse)
                {
                    return -1;
                }
                return 0;
            }
            else if (superSet)
            {
                if (OutputAmount * OutputItem.Price > other.OutputAmount * other.OutputItem.Price
                    || other.Inputs.Keys.Any(k => Inputs[k] < other.Inputs[k]))
                {
                    return null;
                }
                return -1;
            }
            else if (subSet)
            {
                if (OutputAmount * OutputItem.Price < other.OutputAmount * other.OutputItem.Price 
                    || Inputs.Keys.Any(k => Inputs[k] > other.Inputs[k]))
                {
                    return null;
                }
                return 1;
            }
            return null;
        }
        //public bool HasInput(IItem item) => Inputs.ContainsKey(item);
        public double Profit(double output) => OutputItem.Price * output;
        public bool HasOutput(Dictionary<Item, List<double>> inputs)
            => Inputs.IsSubSetOf(inputs);
        public double MaxOutput(Dictionary<Item, List<double>> inputs)
            => OutputAmount * Inputs.Min(i => inputs[i.Key][^1] / i.Value);

        private readonly Item _OutputItem;

        public MultiProcess(
            Processor processor,
            Dictionary<Item, int> inputs,
            Item outputItem,
            double outputAmount,
            ICondition[] conditions = null)
            : base(true, conditions)
        {
            Source = processor;
            Inputs = inputs;
            _OutputItem = outputItem;
            OutputAmount = outputAmount;
        }
    }
}
