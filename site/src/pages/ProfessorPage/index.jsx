import React, { useState, useEffect } from 'react'
import axios from 'axios';
import ProfSideInfo from '../../component/ProfSideInfo/ProfSideInfo';
import Schedule from '../../component/Schedule/Schedule';
import SubReview from '../../component/Review/SubReview';
import GradeDist from '../../component/GradeDist/GradeDist';

const ProfessorPage = (props) => {
  const [profData, setProfData] = useState(null);
  const [schedCourse, setSchedCourse] = useState(null);
  const [reviews, setReviews] = useState([]);
  const [gradeCourse, setGradeCourse] = useState(null);
  const [gradeCourseData, setGradeCourseData] = useState(null);
  var profName = ""
    const fetchDataFromApi = async () => {
        const apiResponse = await axios.get('/professors/api/' + props.match.params.id);
        setProfData(apiResponse.data);
        profName = apiResponse.data.name
        console.log(profName)
    }
    const fetchReviews = async () => {
      const res = await axios.get(`/reviews/?professorID=${profData.ucinetid}`);
      const data = res.data.data.reviewsByProfessorID.data.filter((review) => review !== null);
      setReviews(data);
    }
    const fetchCourseData = async (courseID) => {
      const id = courseID.replace(/\s/g,'');
      const res = await axios.get(`/courses/api/${id}`);
      setGradeCourse(id);
      setGradeCourseData(res.data);
    }

    useEffect(() => {
        fetchDataFromApi();
    }, []);
    useEffect(() => {
      if (profData) {
        fetchReviews();
      }
    }, [profData])

  return (
    <div>
      <section style={{position: "sticky", top: "4rem", height: "min-content", width: "340px", border: "1px solid #EEEEEE", borderRadius: "10px"}}>
          <ProfSideInfo {...profData} />
      </section>
      <h3>Schedule of Classes</h3>
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
      {schedCourse && <Schedule key={schedCourse} course={schedCourse} />}
      <h3>Grade Distribution</h3>
      {profData && profData.course_history.length != 0 && <>
        <select name="courses" id="courses" onChange={(event) => {
          fetchCourseData(event.target.value);
        }}>
          <option value="" selected disabled hidden>Course:</option>
          {profData.course_history.map((e) => 
            <option value={e}>{e}</option>
          )}
        </select>
      </>}
      {gradeCourseData && <GradeDist key={gradeCourse} {...gradeCourseData} currentProf={profName}/>}
      <h3>Reviews</h3>
      {reviews.map((review, i) => {
          if (review !== null) return (<SubReview review={review} key={i}/>)
      })}
    </div>
  )
}

export default ProfessorPage