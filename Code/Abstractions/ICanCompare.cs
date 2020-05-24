using System;
using System.Collections.Generic;

namespace StardewValleyStonks
{
    public interface ICanCompare<T> : IComparable<T>
    {
        public bool CanCompareTo(T other);
    }

    public static class CanCompareExtentions
    {
        public static void DoComparisons<T>(this LinkedList<T> list, Dictionary<T, List<T>> equalDict)
            where T: ICanCompare<T>
        {
            LinkedListNode<T> fert = list.First;
            while (fert != list.Last)
            {
                LinkedListNode<T> other = fert.Next;
                while (other != list.Last)
                {
                    if (fert.Value.CanCompareTo(other.Value))
                    {
                        int comparisson = fert.Value.CompareTo(other.Value);
                        if (comparisson == 0)
                        {
                            equalDict[fert.Value].Add(other.Value);
                            equalDict.Remove(other.Value);
                            list.Remove(other);
                        }
                        else if (comparisson == 1)
                        {
                            equalDict.Remove(other.Value);
                            list.Remove(other);
                        }
                        else
                        {
                            equalDict.Remove(fert.Value);
                            list.Remove(fert);
                            break;
                        }
                    }
                    other = other.Next;
                }
                fert = fert.Next;
            }
        } 
    }
}
