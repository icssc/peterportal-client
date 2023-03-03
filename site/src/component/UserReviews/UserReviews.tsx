import axios, { AxiosResponse } from "axios";
import React, { FC, useEffect, useState } from "react";
import SubReview from '../../component/Review/SubReview';
import Button from 'react-bootstrap/Button';
import { Divider } from 'semantic-ui-react';
import { ReviewData } from "src/types/types";
import './UserReviews.scss';
import { useCookies } from "react-cookie";

const UserReviews: FC = () => {
    const [reviews, setReviews] = useState<ReviewData[]>([]);
    const [loaded, setLoaded] = useState<boolean>(false);
    const [cookies, setCookie] = useCookies(['user']);

    const getUserReviews = async () => {
        const response: AxiosResponse<ReviewData[]> = await axios.get(`/reviews?userID=${cookies.user.id}`);
        setReviews(response.data);
        setLoaded(true);
    }

    useEffect(() => {
        getUserReviews();
    }, []);

    const deleteReview = async (reviewID: string) => {
        await axios.delete('/reviews', { data: { id: reviewID } });
        setReviews(reviews.filter(review => review._id !== reviewID));
    }

    if (!loaded) {
        return <p>Loading...</p>;
    } else if (reviews.length === 0) {
        return <p>No reviews to display at the moment.</p>;
    } else {
        return <div className='user-reviews-container'>
            <h1>Your Reviews</h1>
            <p>Deleting a review will remove it permanently.</p>
            {
                reviews.map((review, i) => <div key={`user-reviews-${i}`} className='user-reviews'>
                    <Divider />
                    <SubReview review={review}></SubReview>
                    <div className='user-reviews-footer'>
                        <Button variant='danger' className='mr-3' onClick={() => deleteReview(review._id!)}>Delete</Button>
                    </div>
                </div>)
            }
        </div>
    }
};

export default UserReviews;