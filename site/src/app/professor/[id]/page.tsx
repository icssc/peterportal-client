import ProfessorPage from '../ProfessorPage';

export async function generateMetadata({ params }: { params: Promise<{ id: string }> }) {
  const id = decodeURIComponent((await params).id);
  return {
    title: `${id} | PeterPortal`,
    description: `View offered courses, grade distributions, and reviews for Professor ${id} at UC Irvine.`,
  };
}

interface ProfessorPageParams {
  params: Promise<{ id: string }>;
}
const Page = async ({ params }: ProfessorPageParams) => {
  return <ProfessorPage ucinetid={decodeURIComponent((await params).id)} />;
};
export default Page;
