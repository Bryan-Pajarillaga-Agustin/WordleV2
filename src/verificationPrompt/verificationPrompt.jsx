import "./verificationPrompt.css"
export default function VerificationPrompt({Hint, showVerificationPrompt, setShowVerificationPrompt}){
    return(
        <div className={ showVerificationPrompt ? "VerificationPrompt" : "unShowVerificationPrompt"}>
            <h1>Would You Take A Hint?</h1>

            <div className="display_Flex">
                <button className="options" onClick={()=>setShowVerificationPrompt(false)}>No</button>
                <button className="options" onClick={()=>{Hint("true")}}>Yes</button>
            </div>
        </div>  
    )
}