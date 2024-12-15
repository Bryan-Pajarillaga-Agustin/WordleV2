import "./TopComponents.css"
export default function TopComponents({gold, score, setShowCollection, Hint}){
    return(
        <div className="TopComponents">
            <div className="leftContents">
                <button className="GuessedWords" onClick={()=>{setShowCollection(true)}}>Collection</button>
                <h3>Gold: {gold} <div className="goldIconWrapper"><div className="gold gold1"></div><div className="gold gold2"></div></div></h3>
            </div>
            <div className="middleContents">
                <h2 id="Score">Score: {score}</h2>
            </div>
            <div className="rightContents">
                <button className="HintButton" onClick={()=>Hint()}>Hint</button>
            </div>
        </div>
    )
}