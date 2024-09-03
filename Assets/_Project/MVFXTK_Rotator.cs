using System.Collections;
using System.Collections.Generic;
using UnityEngine;

namespace MVFXTK
{
    public class MVFXTK_Rotator : MonoBehaviour
    {
        public Vector3 rotation;
        public Space space = Space.Self;

        void Start()
        {

        }

        void Update()
        {
            //transform.Rotate(rotation * Time.deltaTime, space);

            if (space == Space.Self)
            {
                transform.localEulerAngles += rotation * Time.deltaTime;
            }
            else
            {
                transform.eulerAngles += rotation * Time.deltaTime;
            }
        }
    }
}