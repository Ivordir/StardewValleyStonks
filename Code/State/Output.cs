using System;
using System.Collections.Generic;
using System.Linq;
using ExtentionsLibrary.Memoization;

namespace StardewValleyStonks
{
    public class Output
    {
        //StaticCrop[] Crops;
        //StaticRegrowCrop[] RegrowCrops;
        //StaticFertilizer[] Fertilizers;
        //List<PlanNode> Plans;
        private int FarmBuffLevel;
        private readonly Fertilizer NoFertilizer = new Fertilizer("None", 0, 0, 0);
        private Dictionary<Fertilizer, List<Fertilizer>> EqualAlternates;
        private Fertilizer[] Fertilizers;

        public Output()
        {

        }

        public void Calculate(Data data)
        {
            //make sure NoFert is in list
            LinkedList<Fertilizer> fertilizers = new LinkedList<Fertilizer>
                (data.Fertilizers.Select(f => new Fertilizer(f)));
            EqualAlternates = fertilizers.ToDictionary
                (f => f, f => new List<Fertilizer>());

            LinkedListNode<Fertilizer> fert = fertilizers.First;
            while (fert != fertilizers.Last)
            {
                LinkedListNode<Fertilizer> other = fert.Next;
                while (other != fertilizers.Last)
                {
                    if (fert.Value.CanCompare(other.Value))
                    {
                        int comparisson = fert.Value.CompareTo(other.Value);
                        if (comparisson == 0)
                        {
                            EqualAlternates[fert.Value].Add(other.Value);
                            fertilizers.Remove(other);
                        }
                        else if (comparisson == 1)
                        {
                            fertilizers.Remove(other);
                        }
                        else
                        {
                            fertilizers.Remove(fert);
                            break;
                        }
                    }
                    other = other.Next;
                }
                fert = fert.Next;
            }
            /*
            for(int i = 0; i < fertilizers.Count; i++)
            {
                Fertilizer a = fertilizers[i];
                for(int j = i + 1; j < fertilizers.Count; j++)
                {
                    Fertilizer b = fertilizers[j];
                    if(a.Price == b.Price && a.Quality == b.Quality && a.Speed == b.Speed)
                    {
                        EqualAlternates[a].Add(b);
                        EqualAlternates.Remove(b);
                        fertilizers.RemoveAt(j);
                        j--;
                    }
                    else
                    {
                        bool aIsBetter = a.Price <= b.Price;
                        if ((a.Quality >= b.Quality) == aIsBetter && (a.Speed >= b.Speed) == aIsBetter)
                        {
                            if (aIsBetter)
                            {
                                EqualAlternates.Remove(b);
                                fertilizers.RemoveAt(j);
                                j--;
                            }
                            else
                            {
                                EqualAlternates.Remove(a);
                                fertilizers.RemoveAt(i);
                                i--;
                                break;
                            }
                        }
                        //else cannot compare, some values are better and some are worse than the other fertilizer
                    }
                }
            }
            Fertilizers = fertilizers.ToArray();
            */
        }

        private static Func<int, int, double[]> QualityDistribution =
            new Func<int, int, double[]>((fertQuality, FarmBuffLevel) =>
            {
                double[] dist = new double[3];
                dist[2] = 0.01 + 0.2 * (FarmBuffLevel / 10.0 + fertQuality * (FarmBuffLevel + 2) / 12.0);
                dist[1] = Math.Min(2 * dist[2], 0.75) * (1 - dist[2]);
                dist[0] = 1 - dist[1] - dist[2];
                return dist;
            }).Memoize();

        private void CompareFertilizers(Fertilizer a, Fertilizer b)
        {

        }
    }
}
