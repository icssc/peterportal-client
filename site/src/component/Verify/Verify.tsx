import axios, { AxiosResponse } from "axios";
import React, { FC, useEffect, useState } from "react";
import SubReview from '../../component/Review/SubReview';
import Button from 'react-bootstrap/Button';
import { Divider } from 'semantic-ui-react';
import { ReviewData } from "src/types/types";
import './Verify.scss';

const Verify: FC = () => {
    const [reviews, setReviews] = useState<ReviewData[]>([]);
    const [loaded, setLoaded] = useState<boolean>(false);

    interface AdminResponse {
        admin: boolean
    }

    const getUnverifiedReviews = async () => {
        const response: AxiosResponse<ReviewData[]> = await axios.get('/reviews?verified=false');
        setReviews(response.data);
        setLoaded(true);
    }

    useEffect(() => {
        getUnverifiedReviews();
    }, []);

    const verifyReview = async (reviewID: string) => {
        await axios.patch('/reviews/verify', { id: reviewID });
        getUnverifiedReviews();
    }

    const deleteReview = async (reviewID: string) => {
        await axios.delete('/reviews', { data: { id: reviewID } });
        getUnverifiedReviews();
    }

    if (!loaded) {
        return <p>Loading...</p>;
    } else if (reviews.length === 0) {
        return <p>No reviews to display at the moment.</p>;
    } else {
        return <div className='verify-container'>
            <h1>Unverified Reviews</h1>
            <p>Verifying a review will display the review on top of unverified reviews.</p>
            <p>Deleting a review will remove it permanently.</p>
            {
                reviews.map((review, i) => <div key={`verify-${i}`} className='verify'>
                    <Divider />
                    <SubReview review={review}></SubReview>
                    <div className='verify-footer'>
                        <Button variant='danger' className='mr-3' onClick={() => deleteReview(review._id!)}>Delete</Button>
                        <Button variant='success' onClick={() => verifyReview(review._id!)}>Verify</Button>
                    </div>
                </div>)
            }
        </div>
    }
};

export default Verify;