import React, { useState, useEffect } from 'react'
import axios from 'axios';
import ProfSideInfo from '../../component/ProfSideInfo/ProfSideInfo';

const RoadmapPage = (props) => {
  const [profData, setProfData] = useState(null);

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
    </div>
  )
}

export default RoadmapPage