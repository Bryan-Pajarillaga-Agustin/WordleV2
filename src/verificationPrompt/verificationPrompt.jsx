import "./verificationPrompt.css"
export default function VerificationPrompt({Hint}){
    return(
        <button onClick={()=>Hint(true)}>Yes</button>
    )
}