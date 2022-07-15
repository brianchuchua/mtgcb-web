import Typography from '@material-ui/core/Typography';
import { ResponsiveContainer } from '../components/layout/ResponsiveContainer';

const ChangelogPage: React.FC = () => (
  <ResponsiveContainer maxWidth="xl">
    <Typography variant="h3" component="h1" align="center">
      Changelog
    </Typography>
    <Typography variant="h5" component="h2">
      2022-07-14 (v0.42.1)
    </Typography>
    <Typography paragraph>
      <ul>
        <li>Fixed a bug that caused the card gallery to fully reset when a card quantity was updated.</li>
      </ul>
    </Typography>
    <Typography variant="h5" component="h2">
      2022-07-12 (v0.42.0)
    </Typography>
    <Typography paragraph>
      <ul>
        <li>
          Added filters and sorting for cards at the root-level collection page. (These were previously only available within a set page.)
        </li>
      </ul>
    </Typography>
    <Typography variant="h5" component="h2">
      2022-07-10 (v0.41.0)
    </Typography>
    <Typography paragraph>
      <ul>
        <li>Added louder success pop-up when a card is updated outside the Edit Cards page.</li>
        <li>Added error pop-ups if card updating fails due to an API outage.</li>
      </ul>
    </Typography>
    <Typography variant="h5" component="h2">
      2022-07-04 (v0.40.0)
    </Typography>
    <Typography paragraph>
      <ul>
        <li>Improved loading states / added image placeholders / reduced layout shift.</li>
        <li>Added prefetching logic so the next page in pagination is fetched in advance.</li>
        <li>Made set icons clickable to enter set pages.</li>
      </ul>
    </Typography>
    <Typography variant="h5" component="h2">
      2022-06-22 (v0.39.4)
    </Typography>
    <Typography paragraph>
      <ul>
        <li>Improved caching behavior for collection updates.</li>
      </ul>
    </Typography>
    <Typography variant="h5" component="h2">
      2022-06-21 (v0.39.3)
    </Typography>
    <Typography paragraph>
      <ul>
        <li>Fixed broken Buy links on mobile Safari.</li>
      </ul>
    </Typography>
    <Typography variant="h5" component="h2">
      2022-06-20 (v0.39.2)
    </Typography>
    <Typography paragraph>
      <ul>
        <li>Fixed scrolling bug on MacOS Safari.</li>
      </ul>
    </Typography>
    <Typography variant="h5" component="h2">
      2022-06-19 (v0.39.1)
    </Typography>
    <Typography paragraph>
      <ul>
        <li>Added expansion-level filters and sorting for cards.</li>
        <li>New filtering options:</li>
        <ul>
          <li>Quantity (All) - Foils and non-foils between any quantities you set.</li>
          <li>Quantity (Normal) - Non-foils between any quantities you set.</li>
          <li>Quantity (Foil) - Traditional foils in any quantities you set.</li>
        </ul>
        <li>New sorting options:</li>
        <ul>
          <li>Quantity (All)</li>
          <li>Quantity (Normal)</li>
          <li>Quantity (Foil)</li>
        </ul>
        <li>Fixed a bug where expansion-level searches were broken.</li>
      </ul>
    </Typography>
    <Typography variant="h5" component="h2">
      2022-05-30 (v0.38.0)
    </Typography>
    <Typography paragraph>
      <ul>
        <li>Added collection set filters and sorting.</li>
        <li>New filtering options:</li>
        <ul>
          <li>All - Show all sets.</li>
          <li>Complete - Show sets at 100% completion.</li>
          <li>Partial - Show sets between 1% and 99% completion.</li>
          <li>Empty - Show sets at 0% completion.</li>
        </ul>
        <li>New sorting options:</li>
        <ul>
          <li>Current Value - Sort by current value.</li>
          <li>Cost to Complete - Sort by cost to complete.</li>
          <li>% Collected - Sort by % collected.</li>
        </ul>
      </ul>
    </Typography>
    <Typography variant="h5" component="h2">
      2022-04-10 (v0.37.3)
    </Typography>
    <Typography paragraph>
      <ul>
        <li>Full mobile optimization pass for every site component.</li>
      </ul>
    </Typography>
    <Typography variant="h5" component="h2">
      2022-04-04 (v0.37.2)
    </Typography>
    <Typography paragraph>
      <ul>
        <li>Fixed a bug with case-sensivity during account migration.</li>
      </ul>
    </Typography>
    <Typography variant="h5" component="h2">
      2022-04-03 (v0.37.1)
    </Typography>
    <Typography paragraph>
      <ul>
        <li>Changed some components to use native versions on mobile! (More of these to come.)</li>
        <li>Made the MTG CB beta public!</li>
      </ul>
    </Typography>
    <Typography variant="h5" component="h2">
      2022-03-10 (v0.36.0)
    </Typography>
    <Typography paragraph>
      <ul>
        <li>Added Edit Cards page!</li>
      </ul>
    </Typography>
    <Typography variant="h5" component="h2">
      2022-03-03 (v0.35.1)
    </Typography>
    <Typography paragraph>
      <ul>
        <li>Fixed card quantities being overriden during parallel updates.</li>
      </ul>
    </Typography>
    <Typography variant="h5" component="h2">
      2022-02-20 (v0.35.0)
    </Typography>
    <Typography paragraph>
      <ul>
        <li>Added ability to update collection in set and collection views.</li>
      </ul>
    </Typography>
    <Typography variant="h5" component="h2">
      2022-01-11 (v0.34.0)
    </Typography>
    <Typography paragraph>
      <ul>
        <li>Allowed mobile sidenav to close when the user presses the region outside of it.</li>
      </ul>
    </Typography>
    <Typography variant="h5" component="h2">
      2022-01-09 (v0.33.0)
    </Typography>
    <Typography paragraph>
      <ul>
        <li>Made the site usable on mobile devices with a dozen related UI changes.</li>
        <li>Reverted the browse view to default to viewing sets first.</li>
        <li>Removed breadcrumbs from the top of pages -- they were just taking up space redundantly.</li>
      </ul>
    </Typography>
    <Typography variant="h5" component="h2">
      2022-01-03 (v0.32.2)
    </Typography>
    <Typography paragraph>
      <ul>
        <li>
          Fixed a bug causing blank search results due to filters changing after the user navigated to a further page of a previous search.
          The page will properly reset back to 1 now.
        </li>
        <li>Fixed a bug where the Collection menu item was not always active when viewing your collection.</li>
      </ul>
    </Typography>
    <Typography variant="h5" component="h2">
      2022-01-02 (v0.32.1)
    </Typography>
    <Typography paragraph>
      <ul>
        <li>
          Fixed several client-side errors reported in Sentry.io -- viewing a table "too quickly" would cause a crash if data weren't
          available for it yet.
        </li>
      </ul>
    </Typography>
    <Typography variant="h5" component="h2">
      2022-01-01 (v0.32.0)
    </Typography>
    <Typography paragraph>
      <ul>
        <li>Fixed a bug where unique cards completed weren't being calculated properly on the Set collection view.</li>
        <li>Made it clearer which pages are still not yet implemented.</li>
        <li>Corrected percentage rounding issue. (281/282 cards is 99%, not 100%.)</li>
        <li>Fixed the login issue on mobile devices / WebKit browsers.</li>
        <li>Changed the site and API to live on the mtgcollectionbuilder.com domain.</li>
        <li>Set up a redirect from the old domain to the new domain.</li>
      </ul>
    </Typography>
  </ResponsiveContainer>
);

export default ChangelogPage;
