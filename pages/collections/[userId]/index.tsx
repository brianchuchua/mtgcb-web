import { useRouter } from 'next/router';
import { ResponsiveContainer } from '../../../components/layout/ResponsiveContainer';
import { Collection } from '../../../features/collections/Collection';

const CollectionPage: React.FC = () => {
  const router = useRouter();
  const { userId } = router.query;
  const id = userId as string;

  return <ResponsiveContainer maxWidth="xl">{id ? <Collection userId={id} /> : <></>}</ResponsiveContainer>;
};

export default CollectionPage;
