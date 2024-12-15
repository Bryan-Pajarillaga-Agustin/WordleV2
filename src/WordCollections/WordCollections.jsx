import './WordCollections.css'
export default function WordCollections({showCollections, collections, setShowCollection}){
    return (
        <div className={showCollections ? "Collections" : "HideCollections"}>
            <button className="unShowCollection" onClick={()=>setShowCollection(false)}>X</button>
            <h1>Collections</h1>
            <div className="Wrapper-Words">
                <ul>
                    {collections.length != 0 ? collections.map((ea)=><li key={ea}>{ea}</li>) : console.log("error")}
                </ul> 
            </div>
            
        </div>
    )
}