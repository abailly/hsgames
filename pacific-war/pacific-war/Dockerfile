# Dockerfile for E2E testing
# Uses pre-built binary from package.sh (cargo zigbuild)
FROM debian:bookworm-slim

# Install runtime dependencies
RUN apt-get update && apt-get install -y \
    ca-certificates \
    libssl3 \
    curl \
    postgresql-client \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /app

# Copy the pre-built package
# This assumes package.sh has been run to create pacific-war.tar.gz
COPY pacific-war.tar.gz /tmp/

# Extract the package
RUN tar -xzf /tmp/pacific-war.tar.gz -C /app && \
    rm /tmp/pacific-war.tar.gz && \
    chmod +x /app/pacific-war

# Copy migrations (not included in package.sh tarball)
COPY migrations ./migrations

# Expose the port
EXPOSE 8000

# Set environment variables
ENV ROCKET_ADDRESS=0.0.0.0
ENV ROCKET_PORT=8000

# Run the application
CMD ["/app/pacific-war"]
