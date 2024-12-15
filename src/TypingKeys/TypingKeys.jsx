export default function TypingKeys({Keys, KeyType, ArrayOfTypingKeys, CheckAnswer}){

    return(
        <div className="Keyboard_Wrapper">
            <div className="Row-Keys">
                {Keys[0].map((key, i)=>(
                    <li key={key} ref={ArrayOfTypingKeys[0][i]} onClick={()=>KeyType(key)}>
                        {key}
                    </li>
                ))}
            </div>
            <div className="Row-Keys">
                {Keys[1].map((key, i)=>(
                    <li key={key} onClick={()=>KeyType(key)} ref={ArrayOfTypingKeys[1][i]}>
                        {key}
                    </li>
                ))}
            </div>
            <div className="Row-Keys">
                {Keys[2].map((key, i)=>(
                    <li key={key} onClick={()=>KeyType(key)} ref={ArrayOfTypingKeys[2][i]}>
                        {key}
                    </li>
                ))}
                <li onClick={()=>CheckAnswer("Enter")} id="Enter">Enter</li>
            </div>
        </div>
    )
}