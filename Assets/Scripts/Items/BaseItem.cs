using System;
using System.Collections;
using System.Collections.Generic;
using UnityEditor;
using UnityEngine;

namespace OpenWorldTemplate 
{
    [CreateAssetMenu(fileName = "Item", menuName = "OpenWorldTemplate/Items/Item")]
    public class BaseItem : ScriptableObject
    {
        [Header("Details")]
        public string itemID = Guid.NewGuid().ToString();
        public string baseName;
        public string displayName;
        public string description;

        [Header("Values")]
        public ItemRarity itemRarity;
        public int maxStackCount;
        public int sellValue;

        [Header("Assets")]
        public GameObject worldItemPrefab;
        public Sprite itemSprite;

        public virtual void OnValidate()
        {
            ItemRegistry.Register(this);

#if UNITY_EDITOR
            if (string.IsNullOrEmpty(itemID) || itemID == Guid.Empty.ToString())
                itemID = Guid.NewGuid().ToString();

            if (!PrefabUtility.IsPartOfPrefabAsset(worldItemPrefab))
                Debug.LogError($"{this} {worldItemPrefab} is a GameObject - set to prefab!");
#endif
        }
    }

    public enum ItemRarity
    {
        Common = 0,
        Uncommon = 1,
        Rare = 2,
        Epic = 3,
        Legendary = 4,
        Mythic = 5
    }
}
