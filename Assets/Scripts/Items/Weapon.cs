using System.Collections;
using System.Collections.Generic;
using UnityEngine;

namespace OpenWorldTemplate
{
    [CreateAssetMenu(fileName = "Weapon", menuName = "OpenWorldTemplate/Items/Weapon")]
    public class Weapon : BaseItem
    {
        [Header("Weapon Assets")]
        public GameObject itemHeldPrefab;

        [Header("Weapon Data")]
        public float baseDamage;
        public float critDamage;
        public float attackBonus;
        public float defenseBonus;
    }
}
