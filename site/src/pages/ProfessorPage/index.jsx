import React, { useState, useEffect } from 'react'
import axios from 'axios';
import ProfSideInfo from '../../component/ProfSideInfo/ProfSideInfo';
import Schedule from '../../component/Schedule/Schedule';

const ProfessorPage = (props) => {
  const [profData, setProfData] = useState(null);
  const [schedCourse, setSchedCourse] = useState(null);
  const [update, setUpdate] = useState(false);

    const fetchDataFromApi = async () => {
        const apiResponse = await axios.get('/professors/api/' + props.match.params.id);
        setProfData(apiResponse.data);
    }
    useEffect(() => {
        fetchDataFromApi();
    }, []);

  return (
    <div>
      <section style={{position: "sticky", top: "4rem", height: "min-content", width: "340px", border: "1px solid #EEEEEE", borderRadius: "10px"}}>
          <ProfSideInfo {...profData} />
      </section>
      {profData && profData.course_history.length != 0 && <>
          <select name="courses" id="courses" onChange={(event) => {
            setSchedCourse(event.target.value);
          }}>
            <option value="" selected disabled hidden>Course:</option>
            {profData.course_history.map((e) => 
              <option value={e}>{e}</option>
            )}
          </select>
        </>}
        {schedCourse && <Schedule course={schedCourse} />}
    </div>
  )
}

export default ProfessorPage