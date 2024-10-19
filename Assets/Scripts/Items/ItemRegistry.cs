using System.Collections;
using System.Collections.Generic;
using UnityEditor;
using UnityEngine;

namespace OpenWorldTemplate
{
    [InitializeOnLoad]
    public static class ItemRegistry
    {
        private static Dictionary<string, BaseItem> itemDictionary = new Dictionary<string, BaseItem>();

        static ItemRegistry()
        {
            itemDictionary.Clear();
        }

        public static bool Register(BaseItem item)
        {
            if (itemDictionary.ContainsKey(item.itemID))
            {
                if (itemDictionary[item.itemID].baseName != item.baseName)
                    Debug.LogError($"Two items have duplicate id - in database: {itemDictionary[item.itemID].baseName};" +
                                   $" adding to registry {item.baseName}");

                return false;
            }

            itemDictionary[item.itemID] = item;
            return true;
        }

        public static void Unregister(BaseItem item)
        {
            itemDictionary.Remove(item.itemID);
        }

        public static BaseItem GetItem(string id)
        {
            return itemDictionary.ContainsKey(id) ? itemDictionary[id] : null;
        }
    }
}
